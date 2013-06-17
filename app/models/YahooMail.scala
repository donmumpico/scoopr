package models

case class YahooMail(from: String, subject: String, date: Long)

object YahooMail {

  var mails: Set[YahooMail] = Set()

  def add(mail: YahooMail) {
    this.mails = this.mails + mail
  }

  def findAll: List[YahooMail] = this.mails.toList.sortBy(_.date).reverse

}
