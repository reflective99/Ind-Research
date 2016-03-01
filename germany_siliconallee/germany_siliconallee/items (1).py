# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# http://doc.scrapy.org/en/latest/topics/items.html

import scrapy


class ArticleItem(scrapy.Item):
    link_title = scrapy.Field()
    text = scrapy.Field()
    url = scrapy.Field()
    sentiment = scrapy.Field()
