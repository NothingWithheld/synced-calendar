## Database Setup

After installing Postgres, run:

```
createuser server --pwprompt --superuser
# Enter password server when prompted
createdb server
createdb server_test
```

## Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Build libraries: `stack build`

If you have trouble, refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for additional detail.

## Development
Start the database with:
```
sudo service postgresql start 
```

Start a development server with:

```
stack exec -- yesod devel
```

As your code changes, your site will be automatically recompiled and redeployed to localhost.

## API
- GET `/api/{user_id}/free-times`
	
	Retrieves free time entries for a given user. Response sends in a JSON
	array of data objects with `id`, `userId`, `day`, `from_time`, and `to_time` variables (see below requests for possible values of these variables)
	
	user_id (Int) -> Unique number that identifies a user

- POST `/api/{user_id}/free-times`

	Creates a time entry for a given user. Response sends in a JSON object of the 
	newly created `FreeTimeEntry`.

	user_id (Int) -> Unique number that identifies a user

	**Parameters**
	
	- day (String) -> "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", or "Sunday"
	- from_time (String) -> Written in the format of `HH:MM` using military time
	- to_time (String) -> Written in the format of `HH:MM` using military time
- PUT: `/api/{id}/free-times`

	Updates a time entry w

	**Parameters**
	
	- from_time (String) -> Written in the format of `HH:MM` using military time
	- to_time (String) -> Written in the format of `HH:MM` using military time
- DELETE: `/api/{id}/free-times`

## Tests

```
stack test --flag server:library-only --flag server:dev
```

(Because `yesod devel` passes the `library-only` and `dev` flags, matching those flags means you don't need to recompile between tests and development, and it disables optimization to speed up your test compile times).

## Documentation

* Read the [Yesod Book](https://www.yesodweb.com/book) online for free
* Check [Stackage](http://stackage.org/) for documentation on the packages in your LTS Haskell version, or [search it using Hoogle](https://www.stackage.org/lts/hoogle?q=). Tip: Your LTS version is in your `stack.yaml` file.
* For local documentation, use:
	* `stack haddock --open` to generate Haddock documentation for your dependencies, and open that documentation in a browser
	* `stack hoogle <function, module or type signature>` to generate a Hoogle database and search for your query
* The [Yesod cookbook](https://github.com/yesodweb/yesod-cookbook) has sample code for various needs

## Getting Help

* Ask questions on [Stack Overflow, using the Yesod or Haskell tags](https://stackoverflow.com/questions/tagged/yesod+haskell)
* Ask the [Yesod Google Group](https://groups.google.com/forum/#!forum/yesodweb)
* There are several chatrooms you can ask for help:
	* For IRC, try Freenode#yesod and Freenode#haskell
	* [Functional Programming Slack](https://fpchat-invite.herokuapp.com/), in the #haskell, #haskell-beginners, or #yesod channels.
