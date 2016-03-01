import re
from scrapy.exceptions import DropItem
from goose import Goose
from textblob import TextBlob



class DropSelfPostsPipeline(object):
    def process_item(self, item, spider):
        match = re.match("/news/general.html/\d+", item["url"])
        if match:
            raise DropItem("Excluded self-post: " + item["url"])

        return item

class ExtractArticlePipeline(object):
    def __init__(self):
        self.goose = Goose()

    def process_item(self, item, spider):
        try:
            article = self.goose.extract(url=item["url"])
            item["text"] = article.cleaned_text

        except IndexError:
            raise DropItem("Failed to extract article text from: " + item["url"])

        return item

class SentimentPipeline(object):
    def process_item(self, item, spider):
        blob = TextBlob(item["text"])
        item["sentiment"] = blob.sentiment.polarity
        return item
