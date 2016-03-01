import scrapy
from scrapy.spiders import CrawlSpider, Rule
from scrapy.linkextractors import LinkExtractor
from pytexas.items import PytexasItem
from scrapy.loader import ItemLoader
from scrapy.contrib.linkextractors.sgml import SgmlLinkExtractor

class TalkspiderCrawlSpider(CrawlSpider):
    name = 'talkspider_crawl'
    allowed_domains = ['http://propakistan.pk/']
    start_urls = ['http://propakistani.pk/2015/']

    rules = (
        Rule(LinkExtractor(allow='/2015/'), callback = 'parse_item', follow = True),
    )

    def parse_item(self, response):
        l = ItemLoader(item=PytexasItem(), response = response)
        l.add_xpath('title', '//*/div[@class="span6"]/h2/text()')
        l.add_xpath('speaker', '//*/div[@class="span6"]/h3/text()')
        l.add_xpath('description', '//*/div[@class="span6"]/p[2]/text()')
        return l.load_item()
