package controllers

import scala.concurrent.Future
import play.api.mvc._
import play.api.libs.ws._
import play.api.libs.oauth._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.Play.current
import models.YahooMail
import play.api.Play

object YahooMails extends Controller {

  val jsonRequestMailBody = Json.obj(
    "method" -> "ListMessages",
    "params" -> Json.arr(
      Json.obj(
        "fid" -> "Inbox",
        "numInfo" -> 25,
        "numMid" -> 25,
        "sortKey" -> "date",
        "sortOrder" -> "up",
        "groupBy" -> "unRead")))

  val consumerKey = ConsumerKey(
    current.configuration.getString("mail.yahoo.consumerKey").get,
    current.configuration.getString("mail.yahoo.consumerSecret").get)

  val YahooMailOAuthService = OAuth(ServiceInfo(
    "https://api.login.yahoo.com/oauth/v2/get_request_token",
    "https://api.login.yahoo.com/oauth/v2/get_token",
    "https://api.login.yahoo.com/oauth/v2/request_auth", consumerKey),
    false)

  def sessionTokenPair(implicit request: RequestHeader): Option[RequestToken] = {
    for {
      token <- request.session.get("token")
      secret <- request.session.get("secret")
    } yield {
      RequestToken(token, secret)
    }
  }

  def auth = Action { request =>
    request.queryString.get("oauth_verifier").flatMap(_.headOption).map { verifier =>
      val tokenPair = sessionTokenPair(request).get
      // We got the verifier; now get the access token, store it and back to index
      YahooMailOAuthService.retrieveAccessToken(tokenPair, verifier) match {
        case Right(t) => {
          // We received the authorized tokens in the OAuth object - store it before we proceed
          Redirect(routes.YahooMails.list).withSession("token" -> t.token, "secret" -> t.secret)
        }
        case Left(e) => throw e
      }
    }.getOrElse(
      YahooMailOAuthService.retrieveRequestToken(request.host + routes.YahooMails.auth.toString) match {
        case Right(t) => {
          // We received the unauthorized tokens in the OAuth object - store it before we proceed
          Redirect(YahooMailOAuthService.redirectUrl(t.token)).withSession("token" -> t.token, "secret" -> t.secret)
        }
        case Left(e) => throw e
      })
  }

  def list = Action { request =>
    val tokenPair = sessionTokenPair(request).get
    Async {
      val responseFuture =
        WS.url("http://mail.yahooapis.com/ws/mail/v1.1/jsonrpc")
          .sign(OAuthCalculator(consumerKey, tokenPair))
          .withHeaders("Content-Type" -> "application/json")
          .withHeaders("Accept" -> "application/json")
          .post(jsonRequestMailBody.toString)

      responseFuture.map { response =>
        val responseBody = Json.parse(response.body)
        val mailsArray = mailsReads.reads(responseBody)
        val m = mailsArray.get
        for (mail <- m.value)
          YahooMail.add(mail.as[YahooMail])

        val yahooMails = YahooMail.findAll
        Ok(views.html.yahoomail(yahooMails))
      }
    }

  }

  implicit val mailsReads = (
    (JsPath \ "result" \ "messageInfo").read[JsArray])

  implicit val mailReads: Reads[YahooMail] = (
    (JsPath \ "from" \ "name").read[String] and
    (JsPath \ "subject").read[String] and
    (JsPath \ "receivedDate").read[Long])(YahooMail.apply _)

}