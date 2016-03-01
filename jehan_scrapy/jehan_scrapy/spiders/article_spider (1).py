from scrapy.spiders import CrawlSpider, Rule
from scrapy.linkextractors import LinkExtractor
from jehan_scrapy.items import ArticleItem
from goose import Goose

class ArticleSpider(CrawlSpider):
    name = "articlespider"
    allowed_domains = ["jehanara.wordpress.com"]
    start_urls = [
        "https://jehanara.wordpress.com/"
    ]

    rules = (
        Rule(LinkExtractor(allow=r'jehanara.wordpress.com/page/\d+'), callback="parse_item", follow=True),
    )

    def parse_item(self, response):
        self.log("Scraping: " + response.url)

        articles = response.xpath("//div[@id='content']/div[contains(@class, 'category-posts') and contains(@id, 'post-')]")

        for article in articles:
            item = ArticleItem()
            item["author"] = "Jehan Ara"
            if (article.xpath('h2/a/text()').extract()):
                item["title"] = article.xpath('h2/a/text()').extract()[0]
                if (article.xpath('h2/a/@href').extract()[0]):
                    item["url"] = article.xpath('h2/a/@href').extract()[0]
                    url = article.xpath('h2/a/@href').extract()
                    g = Goose()
                    text = g.extract(url=url[0])
                    item["text"] = text.cleaned_text


            yield item
