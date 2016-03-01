import scrapy
from scrapy.spiders import CrawlSpider, Rule
from scrapy.linkextractors import LinkExtractor
from india_siliconindia.items import ArticleItem


class ArticleSpider(CrawlSpider):
    name = "articlespider"
    allowed_domains = ["siliconindia.com"]
    start_urls = (
        'http://www.siliconindia.com/news/general.html/1',
    )

    rules = (
        Rule(LinkExtractor(allow=r"siliconindia.com/news/general.html/\d+"), callback="parse_item", follow=True),
    )

    def parse_item(self, response):
        self.log("Scraping: " + response.url)

        articles = response.xpath('//a[@class="Verdanabold"]')

        for article in articles:
            item = ArticleItem()
            item["title"] = article.xpath('text()').extract()[0]
            item["url"] = article.xpath('@href').extract()[0]

            yield item
