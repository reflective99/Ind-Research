# -*- coding: utf-8 -*-
import scrapy
from scrapy.spiders import CrawlSpider, Rule
from scrapy.contrib.linkextractors import LinkExtractor
from brazil_zdnet.items import ArticleItem

class ArticleSpider(CrawlSpider):
    name = "articlespider"
    allowed_domains = ["zdnet.com"]
    start_urls = (
        'http://www.zdnet.com/blog/brazil/',
    )

    rules = (
        Rule(LinkExtractor(allow=r'zdnet.com/blog/brazil/\d+'), callback="parse_item", follow=True),
    )

    def parse_item(self, response):
        self.log("Scraping: " + response.url)

        articles = response.xpath("//article[@class='item']/div/div[@class='content']/h3")


        for article in articles:
            item = ArticleItem()
            item["link_title"] = article.xpath('a/text()').extract()[0]
            item["url"] = unicode("http://www.zdnet.com")+article.xpath('a/@href').extract()[0]
            yield item
