# -*- coding: utf-8 -*-
import scrapy
from scrapy.spiders import CrawlSpider, Rule
from scrapy.linkextractors import LinkExtractor
from scrapy.contrib.linkextractors.sgml import SgmlLinkExtractor
from techcrunch.items import ArticleItem

class ArticlesSpider(CrawlSpider):
    name = "articles"
    allowed_domains = "http://techcrunch.com", "https://techcrunch.com", "techcrunch.com", "www.techcrunch.com"
    start_urls = (
        'http://www.techcrunch.com/',
    )

    rules = (Rule(SgmlLinkExtractor(allow=[r'\d{4}/\d{2}/\d{2}/[a-z-0-9]+']), callback="parse_items", follow=True), )


    def parse_items(self, response):
        item = ArticleItem()
        item['title'] = response.xpath('/html/body/div[3]/article/div/div[1]/div/header/h1/text()').extract()
        item['author'] = response.xpath('/html/body/div[3]/article/div/div[1]/div/header/div[2]/div[1]/a/text()').extract()
        item['date'] = response.xpath('/html/body/div[3]/article/div/div[1]/div/header/div[2]/div[1]/time/@datetime').extract()
        ptags = ''
        textBody = response.xpath('//div[@class = "article-entry text"]')
        for text in textBody.xpath('p'):
            ptags += text.extract();
        item['text'] = ptags
        return item
