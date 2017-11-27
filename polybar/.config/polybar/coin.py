#!/usr/bin/env python3

import sys
import os
import requests


class Coin:
    def __init__(self, name, prefix):
        self.name = name
        self.prefix = prefix

    def get_price(self):
        response = requests.get('https://api.coinmarketcap.com/v1/ticker/' +
                                self.name + '/')
        ticker = response.json()

        price = float(ticker[0]['price_usd'])
        return price

    def get_text(self):
        response = requests.get('https://api.coinmarketcap.com/v1/ticker/' +
                                self.name + '/')
        ticker = response.json()

        price = float(ticker[0]['price_usd'])
        change = float(ticker[0]['percent_change_24h'])
        change_text = ''

        if change >= 0:
            change_text = '%{F#98c379}' + ('%.2f' % change) + '%'
        else:
            change_text = '%{F#be5046}' + ('%.2f' % change) + '%'

        text = (coin.prefix + ' $%.2f (%s%%{F-})') % (price, change_text)
        return text


coins = {
    "btc": Coin("bitcoin", "BTC"),
    "bch": Coin("bitcoin-cash", "BCH"),
    "eth": Coin("ethereum", "ETH"),
    "xmr": Coin("monero", "XMR")
}

arg = sys.argv[1]
if arg == "ratio":
    try:
        ratio = float(requests.get('https://shapeshift.io/rate/bch_eth')
                      .json()["rate"])
        last_ratio = 0

        if ratio <= 0:
            quit()

        if not os.path.exists("/tmp/coin_ratio.txt"):
            with open("/tmp/coin_ratio.txt", "w") as tmpfile:
                pass

        with open("/tmp/coin_ratio.txt", "r") as tmpfile:
            lines = tmpfile.readlines()
            if len(lines) > 0:
                last_ratio = float(lines[len(lines)-1])

        with open("/tmp/coin_ratio.txt", "a") as tmpfile:
            if last_ratio != ratio:
                tmpfile.write(str(ratio) + "\n")

        change = (-1 + (ratio/last_ratio)) * 100

        if abs(change) < 0.00001:
            quit()

        change_text = ''

        if change >= 0:
            change_text = '%{F#98c379}' + ('%.2f' % change) + '%'
        else:
            change_text = '%{F#be5046}' + ('%.2f' % change) + '%'

        text = ("BCH/ETH %.3f (%s%%{F-})") % (ratio, change_text)
        print(text)
    except Exception:
        pass
else:
    coin = coins[arg]
    print(coin.get_text())
