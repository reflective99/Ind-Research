import scrapy


class ArticleItem(scrapy.Item):
    link_title = scrapy.Field()
    url = scrapy.Field()
    sentiment = scrapy.Field()
    text = scrapy.Field()
