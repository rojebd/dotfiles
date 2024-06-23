config.load_autoconfig(False)
config.set('content.headers.user_agent', 'Mozilla/5.0 (X11; Linux x86_64; rv:57.0) Gecko/20100101 Firefox/77.0', 'https://accounts.google.com/*')


config.bind('M', 'hint links spawn /home/roniell/.local/bin/tofi-float.sh "{hint-url}"')
config.bind('<', 'hint links spawn /home/roniell/.local/bin/tofi-download.sh "{hint-url}"')

config.unbind('<Shift-j>')
config.unbind('<Shift-k>')
config.bind('<Shift-j>', 'tab-prev')
config.bind('<Shift-k>', 'tab-next')

config.set('downloads.location.directory', '~/')

config.set('url.searchengines', {'DEFAULT': 'https://search.inetol.net/search?q={}', '!hoog': 'https://hoogle.haskell.org/?hoogle={}', '!yt': 'https://www.youtube.com/results?search_query={}', '!g': 'https://google.com/search?q={}'})
config.set("url.start_pages",  "https://search.inetol.net/")
config.set("url.default_page", "https://search.inetol.net/")
#config.set("url.auto_search",  "https://search.inetol.net/")

config.bind('xs', 'config-cycle statusbar.show always never')
config.bind('xx', 'config-cycle tabs.show always never')
# config.bind('xx', 'config-cycle tabs.show always never;; config-cycle statusbar.show always never')

config.set("colors.webpage.darkmode.enabled", True)
config.source("gruvbox.py")
