import scala.concurrent.duration.Duration

object Main extends App {
  gbghack2020.p4.run()
}

/*
curl 'https://api-extern.systembolaget.se/sb-api-ecommerce/v1/productsearch/search?size=30&page=1&categoryLevel1=%C3%96l&categoryLevel2=Vete%C3%B6l&isEcoFriendlyPackage=false&isInDepotStockForFastDelivery=false' \
-H 'authority: api-extern.systembolaget.se' \
-H 'pragma: no-cache' \
-H 'cache-control: no-cache' \
-H 'accept: application/json, text/plain, * /*' \
  -H 'user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.193 Safari/537.36' \
  -H 'ocp-apim-subscription-key: 874f1ddde97d43f79d8a1b161a77ad31' \
  -H 'origin: https://www.systembolaget.se' \
  -H 'sec-fetch-site: same-site' \
  -H 'sec-fetch-mode: cors' \
  -H 'sec-fetch-dest: empty' \
  -H 'referer: https://www.systembolaget.se/' \
  -H 'accept-language: en-US,en;q=0.9,sv;q=0.8,bn;q=0.7' \
  --compressed
   */
 */