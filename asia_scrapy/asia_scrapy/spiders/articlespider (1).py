from scrapy.spiders import CrawlSpider, Rule
from scrapy.linkextractors import LinkExtractor
from asia_scrapy.items import ArticleItem
from textblob import TextBlob


class ArticleSpider(CrawlSpider):
    name = "articlespider"
    allowed_domains = ["e27.co"]
    start_urls = [
        "http://e27.co/news/"
    ]

    rules = (
        Rule(LinkExtractor(allow=r"e27.co/news/channel/\w+"), callback="parse_item", follow=True),
        Rule(LinkExtractor(allow=r"e27.co/news/channel/\w+/page/\d+"), callback="parse_item", follow=True),

    )

    def parse_item(self, response):
        self.log("Scraping: " + response.url)

        articles = response.xpath("//a[@class='dark-anchor']")

        for article in articles:
            item = ArticleItem()
            item["title"] = article.xpath('h2/text()').extract()[0]
            item["url"] = article.xpath('@href').extract()[0]

            yield item
