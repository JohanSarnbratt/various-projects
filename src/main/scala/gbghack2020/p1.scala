package gbghack2020

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.stream.ActorMaterializer
import play.api.libs.json.Json

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._

object p1 {
  val dur = Duration(10, "seconds")
  def await[T](fut: Future[T]) = Await.result(fut, dur)

  def run() = {
    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()
    import system.dispatcher

    val allProds = (1 to 10).flatMap(getProds)
    val a = allProds.toSet
    println(s"a size : ${a.size} avg ${a.toArray.map(_.price).sum/a.size}")
    println(s"allProds size : ${allProds.size} avg ${allProds.map(_.price).sum/allProds.size}")
    println(allProds.length)
    println(allProds)
    println(allProds.map(_.productId).toSet.size)
  }

  def getProds(page: Int)(implicit system: ActorSystem, ec: ExecutionContext) = {
    val r = request(page)

    val responseFuture: Future[HttpResponse] = Http().singleRequest(r)

    val response = Await.result(responseFuture, dur)

    println(response.encoding)
    val decodedResponse = akka.http.scaladsl.coding.Coders.Gzip().decodeMessage(response)

    val entity = await(decodedResponse.entity.toStrict(5.seconds).map(_.data.decodeString("UTF-8")))
    val json = Json.parse(entity)
    implicit val prodReads = Json.reads[Prod]
    implicit val sysReads = Json.reads[SystemShape]
    val sys = json.as[SystemShape]
    println(sys.products.map(_.price).sum)
    println(sys.products)
    sys.products
  }

  case class SystemShape(products: Seq[Prod])
  case class Prod(productId: String, price: Double)

  def request(page: Int) = {
    val httpEntity = HttpEntity.empty(ContentTypes.`application/json`)
    HttpRequest(
      method = HttpMethods.GET,
      uri = s"https://api-extern.systembolaget.se/sb-api-ecommerce/v1/productsearch/search?size=30&page=$page&categoryLevel1=%C3%96l&categoryLevel2=Vete%C3%B6l",
      entity = httpEntity
    ).addHeader(RawHeader("authority", "api-extern.systembolaget.se"))
      .addHeader(RawHeader("ocp-apim-subscription-key", "874f1ddde97d43f79d8a1b161a77ad31"))
      .addHeader(RawHeader("pragma", "no-cache"))
      .addHeader(RawHeader("cache-control", "no-cache"))
      .addHeader(RawHeader("user-agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.193 Safari/537.36"))
      .addHeader(RawHeader("origin", "https://www.systembolaget.se"))
      .addHeader(RawHeader("sec-fetch-site", "same-site"))
      .addHeader(RawHeader("sec-fetch-mode", "cors"))
      .addHeader(RawHeader("sec-fetch-dest", "empty"))
      .addHeader(RawHeader("referer", "https://www.systembolaget.se/"))
      .addHeader(RawHeader("accept-language", "en-US,en;q=0.9,sv;q=0.8,bn;q=0.7"))
  }

}
