from scrapy.spiders import CrawlSpider, Rule
from scrapy.contrib.linkextractors import LinkExtractor
from us_siliconangle.items import ArticleItem


class ArticleSpider(CrawlSpider):
    name = "articlespider"
    allowed_domains = ["siliconangle.com"]
    start_urls = [
        "http://www.siliconangle.com/"
    ]

    rules = (
        Rule(LinkExtractor(allow=r"siliconangle.com/page/\d+"), callback="parse_item", follow=True),
    )

    def parse_item(self, response):
        self.log("Scraping: " + response.url)

        articles = response.xpath("//h4[@class='srp-post-title']")

        for article in articles:
            item = ArticleItem()
            item["title"] = article.xpath('a/@href').extract()[0]
            item["url"] = article.xpath('a/text()').extract()[0]
            article = response.xpath('//div[@class="entry-content"]')
            stri = unicode("")
            for p in article:
                para = p.xpath('p').extract()
                stri = stri + para
            item["text"] = stri


            yield item
