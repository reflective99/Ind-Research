import scrapy
from pytexas.items import PytexasItem

class TalkspiderBasicSpider(scrapy.Spider):
    name = "talkspider_basic"
    allowed_domains = ["http://propakistani.pk"]
    start_urls = ['http://propakistani.pk/2015/10/15/disgruntled-cs-student-hacks-university-website-to-record-protest-against-management/']

    def parse(self, response):
        body = response.xpath('//*[@id="post-75304"]/div[3]/div/div[2]/text()').extract()
