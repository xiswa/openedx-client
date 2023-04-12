# openedx

A web client to communicate with OpenEdX API.

To use it, ensure that your monad satisfies the following constraint:
```
type WithXendit env err m = 
  ( MonadReader env m
  , HasXenditConfig env
  , HasErrorConv err env
  , MonadIO m
  , MonadError err m
  )
```

and ensure that Bulk Enrollment view is enabled in your OpenEdX instance.

## Setting up Oauth2 client
You need to set up an Oauth2 client so that you can communicate with OpenEdX API as documented [here](https://github.com/hastexo/webhook-receiver/#edx-oauth2-client).
You will get Client ID and Client secret that you will need to put in the config file below.

## Testing
The simple test suite expects a configuration file named `config.json`
in the root directory.
The configuration file should contain the following:
```
{
  "client_id": <your openedx client ID>,
  "client_secret": <your openedx client secret>,
  "url": <your OpenEdX URL token>
}
```

To generate openedx client ID and secret, see [here]()
