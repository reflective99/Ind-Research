# -*- coding: utf-8 -*-
import scrapy
from scrapy.spiders import CrawlSpider, Rule
from scrapy.linkextractors import LinkExtractor
from scrapy.contrib.linkextractors.sgml import SgmlLinkExtractor
from propakistani.items import ArticleItem

class ArticlesSpider(CrawlSpider):
    name = "articles"
    allowed_domains = "http://propakistani.pk", "https://propakistani.pk", "propakistani.pk", "www.propakistani.pk"
    start_urls = (
        'http://www.propakistani.pk/',
    )

    rules = (Rule(SgmlLinkExtractor(allow=[r'\d{4}/\d{2}/\d{2}/[a-z-0-9]+']), callback="parse_items", follow=True), )


    def parse_items(self, response):
        item = ArticleItem()
        item['title'] = response.xpath('//*[starts-with(@id , "post")]/div[3]/div/div[1]/h1/a/text()').extract()
        item['author'] = response.xpath('//*[@id="aa_post_dtl_athr_p"]/a/text()').extract()
        item['date'] = response.xpath('//*[@id="aa_post_dtl_date_p"]/text()').extract()
        ptags = ''
        textBody = response.xpath('//*[starts-with(@id , "post")]/div[3]/div/div[2]')
        for text in textBody.xpath('p'):
            ptags += text.extract();
        item['text'] = ptags
        return item
