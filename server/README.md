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
   - On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
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

### FreeTimeEntry

- **GET** `/api/{user_id}/free-times`
  Retrieves free time entries for a given user. Response sends in a JSON
  array of data objects with `id`, `userId`, `day`, `fromTime`, and `toTime` variables (see POST documentation for possible values of these variables)

  user_id (Int) -> unique number that identifies a user

  **Parameters**

  - timezone (Int) -> offset from UTC time (range: -12 to 14 only in hours, not zone name)

- **POST** `/api/{user_id}/free-times`

  Creates a free time entry for a given user. Response sends in a JSON object of the
  newly created `FreeTimeEntry`.

  user_id (Int) -> unique number that identifies a user

  **x-www-form-urlencoded body**

  - day (String) -> "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"
  - timezone (Int) -> offset from UTC time (range: -12 to 14 only in hours, not zone name)
  - from_time (String) -> written in the format of `HH:MM` using military time
  - to_time (String) -> written in the format of `HH:MM` using military time

- **PUT** `/api/{id}/free-times`

  Updates a free time entry given its `id`, which is provided in a GET request.

  id (Int) -> unique number that identifies a `FreeTimeEntry`

  **x-www-form-urlencoded body**

  - day (String) -> "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"
  - timezone (Int) -> offset from UTC time (range: -12 to 14 only in hours, not zone name)
  - from_time (String) -> Written in the format of `HH:MM` using military time
  - to_time (String) -> Written in the format of `HH:MM` using military time

- **DELETE** `/api/{id}/free-times`
  Deletes a free time entry given its `id`, which is provided in a GET request.

  id (Int) -> unique number that identifies a `FreeTimeEntry`

### AvailableTimeEntry

- **GET** `/api/{user_id}/available-times`
  Retrieves available time entries for a given user for a given event. Response sends in a JSON array of data objects with `id`, `userId`, `eventId`, `date`, `fromTime`, and `toTime` variables (see POST documentation for possible values of these variables)

  user_id (Int) -> unique number that identifies a user

  **Parameters**

  - event_id (Int) -> id that corresponds to a ProposedEvent object
  - timezone (Int) -> offset from UTC time (range: -12 to 14 only in hours, not zone name)

- **POST** `/api/{user_id}/available-times`

  Creates an available time entry for a given user and event. User responds to ProposedEvent with availabilites. Response sends in a JSON object of the newly created `AvailableTimeEntry`.

  user_id (Int) -> unique number that identifies a user

  **x-www-form-urlencoded body**

  - event_id (Int) -> id that corresponds to a ProposedEvent object
  - date (String) -> written in the format of `"MM-DD-YYYY"`
  - timezone (Int) -> offset from UTC time (range: -12 to 14 only in hours, not zone name)
  - from_time (String) -> written in the format of `HH:MM` using military time
  - to_time (String) -> written in the format of `HH:MM` using military time

- **PUT** `/api/{id}/available-times`

  Updates an available time entry given its `id`, which is provided in a GET request.

  id (Int) -> Unique number that identifies a `AvailableTimeEntry`

  **x-www-form-urlencoded body**

  - date (String) -> written in the format of `"MM-DD-YYYY"`
  - timezone (Int) -> offset from UTC time (range: -12 to 14 only in hours, not zone name)
  - from_time (String) -> Written in the format of `HH:MM` using military time
  - to_time (String) -> Written in the format of `HH:MM` using military time

- **DELETE** `/api/{id}/available-times`
  Deletes an available time entry given its `id`, which is provided in a GET request.

  id (Int) -> Unique number that identifies a `AvailableTimeEntry`

### ProposedEvent

- **GET** `/api/{user_id}/proposed-event/creator`
  Retrieves proposed events created by `user_id`. Response sends in a JSON array of data objects with `id`, `creatorId`, `recipientId`, `name`, `description`, `fromDate`, `toDate`, `confirmed` variables (see POST documentation for possible values of these variables)

  user_id (Int) -> unique number that identifies a user

- **GET** `/api/{user_id}/proposed-event/recipient`
  Retrieves proposed events received by `user_id`. Response sends in a JSON array of data objects with `id`, `creatorId`, `recipientId`, `name`, `description`, `fromDate`, `toDate`, `confirmed` variables (see POST documentation for possible values of these variables)

  user_id (Int) -> unique number that identifies a user

- **POST** `/api/{user_id}/proposed-event/creator`

  Creates a ProposedEvent for a given creator and recipient. Recipient can POST `AvailableTimeEntry` for given ProposedEvent `id`. Response sends in a JSON object of the newly created `ProposedEvent`.

  user_id (Int) -> unique number that identifies a user

  **x-www-form-urlencoded body**

  - recipient_id (Int) -> unique number that identifies a user
  - from_date (String) -> written in the format of `MM-DD-YYYY`
  - to_date (String) -> written in the format of `MM-DD-YYYY`
  - name (Maybe String) -> optional string parameter to name the event
  - description (Maybe String) -> optional string parameter to describe the event

- **PUT** `/api/{id}/proposed-event/creator`

  Updates a ProposedEvent given its `id`, which is provided in a GET request.

  id (Int) -> Unique number that identifies a `ProposedEvent`

  **x-www-form-urlencoded body**

  - recipient_id (Int) -> unique number that identifies a user
  - from_date (String) -> written in the format of `MM-DD-YYYY`
  - to_date (String) -> written in the format of `MM-DD-YYYY`
  - name (Maybe String) -> optional string parameter to name the event
  - description (Maybe String) -> optional string parameter to describe the event
  - confirmed (Bool) -> (_Experimental_) changes status of ProposedEvent, might get removed
    because GET requests doesn't retreive ProposedEvents that are confirmed as they should
    have a corresponding ConfirmedEvent linked to the ProposedEvent `id`.

- **DELETE** `/api/{id}/proposed-event/creator`
  Deletes a ProposedEvent given its `id`, which is provided in a GET request.

  id (Int) -> Unique number that identifies a `ProposedEvent`

### ConfirmedEvent

- **GET** `/api/{user_id}/confirmed-event/creator`
  Retrieves confirmed events created by `user_id`. Response sends in a JSON array of data objects with `eventId`, `creatorId`, `recipientId`, `name`, `description`, `date`, `fromTime`, `toTime` variables (see POST documentation of ProposedEvent for possible values of these variables)

  user_id (Int) -> unique number that identifies a user

- **GET** `/api/{user_id}/confirmed-event/recipient`
  Retrieves confirmed events received by `user_id`. Response sends in a JSON array of data objects with `eventId`, `creatorId`, `recipientId`, `name`, `description`, `date`, `fromTime`, `toTime` variables (see POST documentation of ProposedEvent for possible values of these variables)

  user_id (Int) -> unique number that identifies a user

- **POST** `/api/{id}/confirmed-event/creator`

  Creates a confirmed event that corresponds to a ProposedEvent `id`. Response sends in a JSON object of the newly created `ConfirmedEvent`. This also marks the corresponding ProposedEvent `confirmed` status to True, so it would no longer show up in GET requests for ProposedEvent.

  id (Int) -> unique number that identifies a `ProposedEvent`

  **x-www-form-urlencoded body**

  - date (String) -> written in the format of `MM-DD-YYYY`
  - timezone (Int) -> offset from UTC time (range: -12 to 14 only in hours, not zone name)
  - from_time (String) -> written in the format of `HH:MM` using military time
  - to_time (String) -> written in the format of `HH:MM` using military time

- **PUT** `/api/{id}/confirmed-event/creator`

  Updates a ConfirmedEvent given its `id` (not the ProposedEvent `event_id`), which is provided in a GET request.

  id (Int) -> Unique number that identifies a `ConfirmedEvent`

  **x-www-form-urlencoded body**

  - date (String) -> written in the format of `MM-DD-YYYY`
  - timezone (Int) -> offset from UTC time (range: -12 to 14 only in hours, not zone name)
  - from_time (String) -> written in the format of `HH:MM` using military time
  - to_time (String) -> written in the format of `HH:MM` using military time

- **DELETE** `/api/{id}/confirmed-event/creator`
  Deletes a ConfirmedEvent given the corresponding ProposedEvent's `id`, which is provided in a GET request of ProposedEvent.

  id (Int) -> Unique number that identifies a `ProposedEvent`

## Tests

```
stack test --flag server:library-only --flag server:dev
```

(Because `yesod devel` passes the `library-only` and `dev` flags, matching those flags means you don't need to recompile between tests and development, and it disables optimization to speed up your test compile times).

## Documentation

- Read the [Yesod Book](https://www.yesodweb.com/book) online for free
- Check [Stackage](http://stackage.org/) for documentation on the packages in your LTS Haskell version, or [search it using Hoogle](https://www.stackage.org/lts/hoogle?q=). Tip: Your LTS version is in your `stack.yaml` file.
- For local documentation, use:
  _ `stack haddock --open` to generate Haddock documentation for your dependencies, and open that documentation in a browser
  _ `stack hoogle <function, module or type signature>` to generate a Hoogle database and search for your query
- The [Yesod cookbook](https://github.com/yesodweb/yesod-cookbook) has sample code for various needs

## Getting Help

- Ask questions on [Stack Overflow, using the Yesod or Haskell tags](https://stackoverflow.com/questions/tagged/yesod+haskell)
- Ask the [Yesod Google Group](https://groups.google.com/forum/#!forum/yesodweb)
- There are several chatrooms you can ask for help:
  _ For IRC, try Freenode#yesod and Freenode#haskell
  _ [Functional Programming Slack](https://fpchat-invite.herokuapp.com/), in the #haskell, #haskell-beginners, or #yesod channels.
