package models

case class XingNews(from: String, subject: String, date: Long)

object XingNews {

  var news: Set[XingNews] = Set()

  def add(news: XingNews) {
    this.news = this.news + news
  }

  def findAll: List[XingNews] = this.news.toList.sortBy(_.date).reverse

}
