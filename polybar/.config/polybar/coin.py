#!/usr/bin/env python3

import requests
import sys


class Coin:
    def __init__(self, name, prefix):
        self.name = name
        self.prefix = prefix

coins = {
    "btc": Coin("bitcoin", "BTC"),
    "bch": Coin("bitcoin-cash", "BCH"),
    "eth": Coin("ethereum", "ETH"),
    "xmr": Coin("monero", "XMR"),
}

coin = coins[sys.argv[1]]

r = requests.get('https://api.coinmarketcap.com/v1/ticker/' + coin.name + '/')
ticker = r.json()

price = float(ticker[0]['price_usd'])
change = float(ticker[0]['percent_change_24h'])
change_text = ''

if change >= 0:
    change_text = '%{F#98c379}' + ('%.2f' % change) + '%'
else:
    change_text = '%{F#be5046}' + ('%.2f' % change) + '%'

print((coin.prefix + ' $%.2f (%s%%{F-})') % (price, change_text))
