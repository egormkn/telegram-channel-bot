# telegram-channel-bot

Telegram bot that can subscribe to channels and forward messages from them. Might be useful for reading news or collecting entertainment content from different sources in one place.

## Building

1) Build [libtdjson](https://github.com/tdlib/td#dependencies) and put it to the `lib/` folder
2) Run `stack build`
3) *Optional*: create `.env` file with settings:
   ```
   API_ID=******
   API_HASH=********************************
   PHONE_NUMBER=************
   ```
4) Execute `run.sh`

## Usage

Available commands:

- `/add <link>` - Add channel to subscriptions list
- `/show` - Print a list of active subscriptions
- `/remove <id>` - Remove channel from subscriptions list

## Contributors

[![](https://sourcerer.io/fame/egormkn/egormkn/telegram-channel-bot/images/0)](https://sourcerer.io/fame/egormkn/egormkn/telegram-channel-bot/links/0)[![](https://sourcerer.io/fame/egormkn/egormkn/telegram-channel-bot/images/1)](https://sourcerer.io/fame/egormkn/egormkn/telegram-channel-bot/links/1)[![](https://sourcerer.io/fame/egormkn/egormkn/telegram-channel-bot/images/2)](https://sourcerer.io/fame/egormkn/egormkn/telegram-channel-bot/links/2)[![](https://sourcerer.io/fame/egormkn/egormkn/telegram-channel-bot/images/3)](https://sourcerer.io/fame/egormkn/egormkn/telegram-channel-bot/links/3)[![](https://sourcerer.io/fame/egormkn/egormkn/telegram-channel-bot/images/4)](https://sourcerer.io/fame/egormkn/egormkn/telegram-channel-bot/links/4)[![](https://sourcerer.io/fame/egormkn/egormkn/telegram-channel-bot/images/5)](https://sourcerer.io/fame/egormkn/egormkn/telegram-channel-bot/links/5)[![](https://sourcerer.io/fame/egormkn/egormkn/telegram-channel-bot/images/6)](https://sourcerer.io/fame/egormkn/egormkn/telegram-channel-bot/links/6)[![](https://sourcerer.io/fame/egormkn/egormkn/telegram-channel-bot/images/7)](https://sourcerer.io/fame/egormkn/egormkn/telegram-channel-bot/links/7)

## License

This project is released under the MIT license.
For more details, take a look at the [LICENSE](LICENSE) file.
