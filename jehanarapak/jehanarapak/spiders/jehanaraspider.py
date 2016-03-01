# -*- coding: utf-8 -*-
import scrapy
from scrapy.spiders import CrawlSpider, Rule
from scrapy.linkextractors import LinkExtractor
from scrapy.contrib.linkextractors.sgml import SgmlLinkExtractor
from jehanarapak.items import JehanarapakItem

class ArticleSpider(CrawlSpider):
    name = "articles"
    allowed_domains = {"http://jehanara.wordpress.com", "https://jehanara.wordpress.com" , "jehanara.wordpress.com"}
    start_urls = (
        'https://jehanara.wordpress.com',
    )

    rules = (Rule(SgmlLinkExtractor(allow=[r'\d+/\d+/\d+/\w+']), callback="parse_items", follow=True), )


    def parse_items(self, response):
        item = JehanarapakItem()
        item['title'] = response.xpath('string(//*[@id="content"]/div/h2/text())').extract()
        item['date'] = response.xpath('//*[starts-with(@id , "post")]/p[1]/em[1]/a/text()').extract()
        ptags = ''
        textBody = response.xpath('//*[starts-with(@id, "post")]')
        for text in textBody.xpath('p'):
            ptags += text.extract()
        item['text'] = ptags
        return item
