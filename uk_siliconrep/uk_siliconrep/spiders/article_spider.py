from scrapy.spiders import CrawlSpider, Rule
from scrapy.contrib.linkextractors import LinkExtractor
from uk_siliconrep.items import ArticleItem

class ArticleSpider(CrawlSpider):
    name = "articlespider"
    allowed_domains = ["siliconroundabout.org.uk"]
    start_urls = [
        "http://siliconroundabout.org.uk/"
    ]

    rules = (
        Rule(LinkExtractor(allow=r'siliconroundabout.org.uk/page/\d+'), callback="parse_item", follow=True),
    )

    def parse_item(self, response):
        self.log("Scraping: "+response.url)

        articles = response.xpath("//div[contains(@id, 'post-')]")
        i = 0
        for article in articles:
            item = ArticleItem()
            item["link_title"] = article.xpath("//h2[@class='entry-title']/a/text()").extract()[i]
            item["url"] = article.xpath("//h2[@class='entry-title']/a/@href").extract()[i]
            i=i+1

            yield item
