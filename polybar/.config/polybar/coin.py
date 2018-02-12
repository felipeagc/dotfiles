#!/usr/bin/env python3

import sys
import requests


if __name__ == "__main__":
    coin1 = sys.argv[1].upper()
    coin2 = sys.argv[2].upper()

    url = f"https://min-api.cryptocompare.com/data/pricemultifull?fsyms={coin1}&tsyms={coin2}"

    try:
        json = requests.get(url).json()
        price = float(json["RAW"][coin1][coin2]["PRICE"])
        change = float(json["RAW"][coin1][coin2]["CHANGEPCT24HOUR"])

        if change >= 0:
            change_text = '%{F#a1b56c}' + ('%.2f' % change) + '%'
        else:
            change_text = '%{F#ab4642}' + ('%.2f' % change) + '%'

        suffix = ""
        prefix = ""

        if coin2 == "USD":
            prefix = "$"
        elif coin2 == "EUR":
            prefix = "€"
        else:
            suffix = coin2

        text = (coin1 + ' ' + prefix +
                '%.2f' + suffix + ' (%s%%{F-})') % (price, change_text)

        print(text)
    except Exception:
        pass
