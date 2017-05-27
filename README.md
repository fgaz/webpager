# webpager

A generic pager for the web.

This library probide both a `wai` `Middleware` and an `Application`.

You can use it by simply serving it with `warp` with only the path of
the file to page as argument, or you can override the default config
(for example for using it to display an IRC log)

