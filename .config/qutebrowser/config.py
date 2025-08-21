import catppuccin

# enable catppuccin theme
catppuccin.setup(c, 'mocha', True)

config.load_autoconfig(False)

# key bindings
config.bind(',M', 'hint links spawn linkhandler {hint-url}')
config.bind(',m', 'spawn linkhandler {url}')
config.bind(',p', 'spawn dmenu_bw')
config.bind('tD', 'config-cycle -p colors.webpage.darkmode.enabled true false')
# Per-URL setting doesn't seem to work?
# config.bind('tD', 'config-cycle -p -u *://{url:host}/* colors.webpage.darkmode.enabled true false')

#bind some stylesheets
config.bind('cm', 'config-list-add content.user_stylesheets "~/.config/qutebrowser/css/serif.css"')
config.bind('Cm', 'config-list-remove content.user_stylesheets "~/.config/qutebrowser/css/serif.css"')

# dark mode
c.colors.webpage.preferred_color_scheme = 'dark'
c.colors.webpage.darkmode.enabled = False
c.colors.webpage.darkmode.algorithm = 'lightness-cielab'
c.colors.webpage.darkmode.policy.images = 'never'
config.set('colors.webpage.darkmode.enabled', False, 'file://*')
c.colors.webpage.darkmode.threshold.background = 100

# autosave session
c.auto_save.session = False
# disable autoplay
c.content.autoplay = False
# set nvim as editor
c.editor.command = ['st','-e','nvim', '{}']
# set downloads dispatcher
c.downloads.open_dispatcher = 'st -e yazi {}'
# cookies
c.content.cookies.accept = 'no-unknown-3rdparty'

# cosmetic tweaks
c.content.user_stylesheets = ["~/.config/qutebrowser/css/none.css", "~/.config/qutebrowser/css/serif.css"]
c.tabs.padding = {'bottom': 4, 'left': 4, 'right': 4, 'top': 4}
c.tabs.indicator.width = 0
c.tabs.title.format = "{audio}{current_title}"
c.fonts.tabs.selected = '10pt CozetteVector'
c.fonts.tabs.unselected = '10pt CozetteVector'

c.completion.height = '40%'

# set default pages
c.url.start_pages = 'file:///home/gatonegro/.config/qutebrowser/start.html'
c.url.default_page = 'file:///home/gatonegro/.config/qutebrowser/start.html'

# enable notifications & protocol handlers
config.set('content.notifications.enabled', True, 'https://web.whatsapp.com/*')
config.set('content.notifications.enabled', True, 'https://mail.proton.me/*')
config.set('content.register_protocol_handler', True, 'https://mail.proton.me/*')

# search engines
c.url.searchengines = {
       'DEFAULT': 'https://search.brave.com/search?q={}',
        'aa': 'https://annas-archive.org/search?q={}&ext=epub',
        'apkg': 'https://archlinux.org/packages/?sort=&q={}',
        'aur': 'https://aur.archlinux.org/packages?K={}',
        'aw': 'https://wiki.archlinux.org/index.php?search={}',
        'az': 'https://amazon.com.mx/s?k={}',
        'b': 'https://search.brave.com/search?q={}',
        'd': 'https://duckduckgo.com/?q={}',
        'g': 'https://google.com/search?hl=en&&udm=14&q={}',
        'gb': 'https://google.com/search?tbm=bks&q={}',
        'gh': 'https://github.com/search?utf8=✓&q={}',
        'gs': 'https://scholar.google.com/scholar?hl=en&q={}',
        'lg': 'https://libgen.st/search.php?req={}&lg_topic=libgen&open=0&view=simple&res=25&phrase=1&column=def',
        'r': 'https://old.reddit.com/r/{}',
        'rt': 'https://rutracker.org/forum/tracker.php?nm={}',
        'sep': 'https://plato.stanford.edu/search/searcher.py?query={}',
        'sp': 'https://open.spotify.com/search/{}',
        'w': 'https://en.wikipedia.org/wiki/Special:Search/{}',
        'yt': 'https://youtube.com/results?search_query={}',
        'ym': 'https://music.youtube.com/search?q={}',
        }

# set ad blocker to use braves adblock and the hosts file
c.content.blocking.method = 'both'
# set ad block lists
c.content.blocking.adblock.lists = ['https://easylist.to/easylist/easylist.txt',
                                    'https://easylist.to/easylist/easyprivacy.txt',
                                    'https://easylist-downloads.adblockplus.org/easylistdutch.txt',
                                    'https://easylist-downloads.adblockplus.org/abp-filters-anti-cv.txt',
                                    'https://www.i-dont-care-about-cookies.eu/abp/',
                                    'https://secure.fanboy.co.nz/fanboy-cookiemonster.txt']
