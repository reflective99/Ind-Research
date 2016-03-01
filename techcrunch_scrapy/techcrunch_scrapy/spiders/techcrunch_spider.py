from scrapy.spiders import CrawlSpider, Rule
from scrapy.linkextractors import LinkExtractor
from techcrunch_scrapy.items import TechCrunchItem

class TechCrunchSpider(CrawlSpider):
    name = "techcrunch"
    allowed_domains = ["techcrunch.com"]
    start_urls = [
        "http://techcrunch.com/page/1"
    ]

    rules = (
        Rule(LinkExtractor(allow=r'techcrunch.com/page/\d+'), callback = "parse_item", follow = True),
    )

    def parse_item(self, response):
        self.log("Scraping: " + response.url)

        articles = response.xpath('//li[@class = "river-block "]')

        for article in range(len(articles)):
            item = TechCrunchItem()
            item['title'] = articles.xpath('//h2[@class = "post-title"]/a/text()').extract()[article]
            item['url'] = articles.xpath('//h2[@class = "post-title"]/a/@href').extract()[article]
            item['author'] = articles.xpath('//div[@class = "byline"]/a/text()').extract()[article]
            item['date'] = articles.xpath('//div[@class = "byline"]/time/@datetime').extract()[article]

            yield item
