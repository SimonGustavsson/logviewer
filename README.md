## LogViewer

Just a simple .NET Core app written in F# that monitors a folder for log files (`*.log`) and streams them to a web browser via WebSockets as and when new lines are appended to the file.

Log entries are expected to be in the format ENT_TYPE\tDATE_TIME\tMessage. Where

ENT_TYPE is `INFO`/`WARNING`/`ERROR`.
DATE_TIME is a ISO 8601 compliant date time.
Example: `WARNING   2018-05-19T02:37:36.2825119Z    Example Log!`

**Word of warning**: This is by no means a production quality application. Just me playing around trying some languages/frameworks (F#, Suave, Fake, Elm). To emphasize this, the web client will only attempt to connect to `ws://127.0.0.1:8080/ws`.

Built on Windows 10 using Visual Studio Code, so YMMV building on other platforms.