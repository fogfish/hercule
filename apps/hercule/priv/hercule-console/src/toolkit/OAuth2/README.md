# OAuth2 with React

The access to all Internet facing data services must be governed by Identity and Access Management and protected with bearer access token. Consumer IAM is implemented using OAuth 2.0 protocol (see RFC 6749). React application decouples concerns of authentication/authorization from business logic using Authorization Code Grant flow (IAM is implemented by trusted components outside of the application). 

The usage of OAuth2 with Authorization Code Grant flow complicates the bootstrap of application:

* Application do not have a valid access token, the **authorization** is required. The application shall redirect user agent to auth url (`https://account.silvere.cloud/?response_type=code&client_id=...&state=...`) before access is granted. 
* Application has a valid access token but user has interrupted the browser session (close browser, refresh pages, etc). The application shall recover the token from local storage.
* Authorization server callback the application with access code that needs to be exchanged for access token before access is granted.


The `OAuth2` is a container component that facilitates the react application integration with identity providers and reports the progress of authentication, shows errors, etc.

```javascript
import { OAuth2 } from 'components/OAuth2'

<OAuth2>
  <App/>
</OAuth2>
```

The `OAuth2` container renders an application only if user has a valid access token. It manages the callback procedure of OAuth2 framework, uses a local storage to persist a bearer access token across browser sessions. The components implements an actions to

* `accessToken()` initiates an application state and obtain access token.
* `authorize()` reset existed token and initiates a authorization session
* `withAuth(...)` executes a call to protected network resource in context of existed access token


## Obtain access token

The behaviour of OAuth 2.0 redirect endpoint is defined by RFC 6749:

>
> The redirection request to the client's endpoint typically results in an HTML document response, processed by the user-agent. If the HTML response is served directly as the result of the redirection request, any script included in the HTML document will execute with full access to the redirection URI and the credentials it contains.
> The client SHOULD NOT include any third-party scripts (e.g., third-party analytics, social plug-ins, ad networks) in the redirection endpoint response.  Instead, it SHOULD extract the credentials from the URI and redirect the user-agent again to another endpoint without exposing the credentials (in the URI or elsewhere).  If third-party scripts are included, the client MUST ensure that its own scripts (used to extract and remove the credentials from the URI) will execute first.
>

A traditional web application implements a dedicated callback end-point that uses Vanilla Javascript to handle code exchange process. This approach ensures that sensitive information is not leaked out. This approach requires a boiler plate code on top of React. Thus handling of OAuth2 protocol is embedded into the application. We recommend to consumer OAuth2 callback parameters before untrusted libraries are loaded.

```javascript
// index.js
import store from 'redux/store.js'

// store.js
import { Actions } from './actions'

...
store.dispatch(Actions.accessToken())
```


## Use access token

The access token is available at redux store.  

```javascript
export const myAction = () => 
   async function(dispatch, getState) {
      ...
      getState().auth.authToken
   }
```

There is an alternative approach that removes a dependency from the structure of the store. It is recommended to use an action `withAuth` that injects a token into network I/O function.

```javascript
const myRequest = () =>
   async (token) => (
      fetch(..., {
         headers: {'Authorization': token}
      })
   )

const myAction = () => 
   async function(dispatch, getState) {
      let response = await dispatch(Actions.withAuth( myRequest() ))
      ...
   }
``` 



## Unauthorized error handling

It is not recommended to define a common behavior that sign user out on every network error. The application shall define action dependent error strategy. Use `try { ... } catch (e) { ... } ` together with `authorize()` to force a user sign-in.

```javascript
const myAction = () => 
   async function(dispatch, getState) {
      try {
         let response = await dispatch(Actions.withAuth( myRequest() ))
         ...
      } catch (e) {
         dispatch(Actions.authorize())
      }
   }
```

