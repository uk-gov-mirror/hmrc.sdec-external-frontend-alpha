/*
 * Copyright 2026 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package controllers

import controllers.actions.IdentifierAction
import forms.models.ThreadReferenceForm
import forms.providers.ThreadReferenceFormProvider
import models.Mode
import models.sdec.ExternalUser
import play.api.Logging
import play.api.data.Form
import play.api.http.Status as HttpStatus
import play.api.i18n.{I18nSupport, Messages}
import play.api.mvc.*
import service.ThreadReferenceServiceAlgebra
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.{AuthConnector, AuthorisedFunctions, Enrolments}
import uk.gov.hmrc.http.{NotFoundException, UpstreamErrorResponse}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.{EnterThreadReferenceView, ThreadReferenceView, UnauthorisedView}
import uk.gov.hmrc.auth.core.retrieve.~

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class EnterThreadReferenceController @Inject() (
    val controllerComponents: MessagesControllerComponents,
    val authConnector: AuthConnector,
    identifierAction: IdentifierAction,
    enterThreadReferenceView: EnterThreadReferenceView,
    formProvider: ThreadReferenceFormProvider,
    threadReferenceView: ThreadReferenceView,
    threadReferenceService: ThreadReferenceServiceAlgebra,
    unauthorisedView: UnauthorisedView
)(using ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with AuthorisedFunctions
    with Logging {

  private val form: Form[ThreadReferenceForm] = formProvider()

  def onPageLoad(
      mode: Mode,
      threadReferenceForm: Form[ThreadReferenceForm] = form
  ): Action[AnyContent] = {
    Action.async { implicit request =>
      implicit val messages: Messages = messagesApi.preferred(request)

      authorised()
        .retrieve(
          Retrievals.externalId and
            Retrievals.nino and
            Retrievals.allUserDetails and
            Retrievals.authorisedEnrolments
        ) { case externalId ~ nino ~ userDetails ~ enrolments =>
          val user = ExternalUser(
            externalId = externalId,
            nino = nino,
            userDetails = userDetails,
            enrolments = enrolments
          )
          Future.successful(
            Ok(
              enterThreadReferenceView(
                user: ExternalUser,
                threadReferenceForm,
                mode
              )
            )
          )
        }
        .recoverWith { e =>
          logger.warn(s"Unauthorised access: ${e.getMessage}")
          Future.successful(Ok(unauthorisedView()))
        }
    }
  }

  def onContinue(mode: Mode): Action[AnyContent] = {
    Action.async { implicit request =>
      authorised()
        .retrieve(
          Retrievals.externalId and Retrievals.nino and Retrievals.allUserDetails and Retrievals.authorisedEnrolments
        ) {
          case externalId ~ nino ~ userDetails ~ enrolments => {
            val formData = form.bindFromRequest()
            val user     = ExternalUser(
              externalId = externalId,
              nino = nino,
              userDetails = userDetails,
              enrolments = enrolments
            )
            formData.value
              .filter(t => formProvider.validateThreadReference(t.reference))
              .fold(
                Future.successful(
                  returnBadRequest(
                    user: ExternalUser,
                    formData,
                    mode
                  )
                )
              )(tr =>
                getThreadInformation(
                  user: ExternalUser,
                  formData,
                  mode,
                  tr
                )
              )
          }
        }
        .recoverWith { e =>
          logger.warn(s"Unauthorised access: ${e.getMessage}")
          Future.successful(Ok(unauthorisedView()))
        }
    }
  }

  private def getThreadInformation(
      user: ExternalUser,
      form: Form[ThreadReferenceForm],
      mode: Mode,
      trForm: ThreadReferenceForm
  )(using Request[?]): Future[Result] =
    threadReferenceService
      .checkThreadReference(trForm.reference)
      .map { thread =>
        Ok(threadReferenceView(mode, ThreadReferenceForm(thread.threadReference)))
      }
      .recover {
        case _: NotFoundException =>
          val formWithError =
            form.withGlobalError(Messages("sdec.enterthreadref.api.notfound"))
          logger.warn(s"Thread Reference Not found: ${trForm.reference}")
          NotFound(
            enterThreadReferenceView(
              user: ExternalUser,
              formWithError,
              mode
            )
          )
        case e: UpstreamErrorResponse if e.statusCode == HttpStatus.NOT_FOUND =>
          logger.warn(s"Thread Reference Not found: ${trForm.reference}")
          val formWithError =
            form.withGlobalError(Messages("sdec.enterthreadref.api.notfound"))
          NotFound(
            enterThreadReferenceView(
              user: ExternalUser,
              formWithError,
              mode
            )
          )
        case ex =>
          val formWithError =
            form.withGlobalError(Messages("sdec.enterthreadref.api.error"))
          logger.error("Failed to retrieve thread information", ex)
          ServiceUnavailable(
            enterThreadReferenceView(
              user: ExternalUser,
              formWithError,
              mode
            )
          )
      }

  private def returnBadRequest(
      user: ExternalUser,
      form: Form[ThreadReferenceForm],
      mode: Mode
  )(using
      request: Request[?]
  ): Result = {
    logger.warn(s"Returning bad request for ${form.value}")
    val formWithError =
      form.withGlobalError(Messages("sdec.enterthreadref.error.problem.message"))
    BadRequest(
      enterThreadReferenceView(
        user: ExternalUser,
        formWithError,
        mode
      )
    )
  }

}
