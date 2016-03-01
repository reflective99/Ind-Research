from scrapy.contrib.spiders import CrawlSpider, Rule
from scrapy.contrib.linkextractors import LinkExtractor
from fr_rudebag.items import ArticleItem


class ArticleSpider(CrawlSpider):
    name = "articlespider"
    allowed_domains = ["rudebaguette.com"]
    start_urls = [
        "https://rudebaguette.com/"
    ]

    rules = (
        Rule(LinkExtractor(allow=r"rudebaguette.com/page/\d+"), callback="parse_item", follow=True),
    )

    def parse_item(self, response):
        self.log("Scraping: " + response.url)

        articles = response.xpath("//header/h2")
        # unicode string for http links
        http = unicode('https://www.rudebaguette.com')
        for article in articles:
            item = ArticleItem()

            item["link_title"] = article.xpath("a/text()").extract()
            item["url"] = nicode("https://zdnet/blog/brazil")+articles[0].xpath('a/@href').extract()[0]
            yield item
