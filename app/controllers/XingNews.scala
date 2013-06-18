package controllers

import scala.concurrent.Future
import play.api.mvc._
import play.api.libs.ws._
import play.api.libs.oauth._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.Play.current


object XingNews extends Controller {

  val consumerKey = ConsumerKey(
    current.configuration.getString("social.xing.consumerKey").get,
    current.configuration.getString("social.xing.consumerSecret").get)

  val XingNewsOAuthService = OAuth(ServiceInfo(
    "https://api.xing.com/v1/request_token",
    "https://api.xing.com/v1/access_token",
    "https://api.xing.com/v1/authorize", consumerKey),
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
      XingNewsOAuthService.retrieveAccessToken(tokenPair, verifier) match {
        case Right(t) => {
          // We received the authorized tokens in the OAuth object - store it before we proceed
          Redirect(routes.XingNews.list).withSession("token" -> t.token, "secret" -> t.secret)
        }
        case Left(e) => throw e
      }
    }.getOrElse(
      XingNewsOAuthService.retrieveRequestToken(request.host + routes.XingNews.auth.toString) match {
        case Right(t) => {
          // We received the unauthorized tokens in the OAuth object - store it before we proceed
          Redirect(XingNewsOAuthService.redirectUrl(t.token)).withSession("token" -> t.token, "secret" -> t.secret)
        }
        case Left(e) => throw e
      })
  }

  def list = Action { request =>
    val tokenPair = sessionTokenPair(request).get
    Async {
      val responseFuture =
        WS.url("https://api.xing.com/v1/users/me/network_feed")
          .sign(OAuthCalculator(consumerKey, tokenPair))
          .withHeaders("Content-Type" -> "application/json")
          .withHeaders("Accept" -> "application/json")
          .get

      responseFuture.map { response =>
        val responseBody = Json.parse(response.body)
        Ok(responseBody.toString)
      }
    }

  }

}