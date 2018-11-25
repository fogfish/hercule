import React from 'react'
import { compose, lifecycle, withState, omitProps, branch } from 'recompact'

//
// Global OAuth2 configuration
const OAUTH2_AUTHORIZE = window.env.OAUTH2_AUTHORIZE
const OAUTH2_TOKEN = window.env.OAUTH2_TOKEN
const OAUTH2_CLIENT_ID = window.env.OAUTH2_CLIENT_ID
const OAUTH2_FLOW_TYPE = window.env.OAUTH2_FLOW_TYPE

//
// 
export const authorize = () => {
  window.localStorage.removeItem('access_token')
  window.localStorage.removeItem('access_token_ttl')
  window.location =
    `${OAUTH2_AUTHORIZE}/?${encode({ client_id: OAUTH2_CLIENT_ID, response_type: OAUTH2_FLOW_TYPE, state: 'none' })}`
}

//
//
export const OAuth2 = Placeholder => Recover =>
  compose(
    withState('exchanging', 'exchange', false),
    withState('unauthorized', 'failed', undefined),
    lifecycle({
      async componentDidMount() {
        accessToken(this.props)
      }
    }),
    branch(({ exchanging }) => exchanging, () => Placeholder),
    branch(({ unauthorized }) => unauthorized != undefined, () => Recover),
  )

//
//
const accessToken = async props => {
  props.exchange(true)
  const oauth2 = decode(window.location.search.substring(1))
  window.history.replaceState({}, document.title, window.location.pathname + window.location.hash)

  if (oauth2.error) {
    props.failed(oauth2.error)
  } else if (oauth2.access_token) {
    const token = accessTokenImplicit(oauth2)
    !isValidToken(token) ? props.failed('unauthorized') : refresh(token, props)
  } else if (oauth2.code) {
    try {
      const token = await accessTokenExchange(oauth2)
      !isValidToken(token) ? props.failed('unauthorized') : refresh(token, props)
    } catch (e) {
      props.failed('unauthorized')
    }
  } else {
    const token = accessTokenStorage()
    !isValidToken(token) ? props.failed('expired') : refresh(token, props) 
  }

  props.exchange(false)
}

//
// encode / decode utilities
const keyval = (key, val) => (key === '' ? val : decodeURIComponent(val))
const decode = text => (text ? JSON.parse(`{"${text.replace(/&/g, '","').replace(/=/g, '":"')}"}`, keyval) : {})
const encode = json =>
  Object.keys(json).map(key => ([encodeURIComponent(key), encodeURIComponent(json[key])].join('='))).join('&')

//
//
const isValidToken = ({ token, expires }) => {
  const now = +new Date()
  return (token && expires > now)
}

//
//
const accessTokenStorage = () => {
  const now = +new Date()
  const token = window.localStorage.getItem('access_token')
  const expires = parseInt(window.localStorage.getItem('access_token_ttl'))
  return { token, expires }
}

//
//
const accessTokenImplicit = (oauth2) => {
  const now = +new Date()
  const token = `Bearer ${oauth2.access_token}`
  const expires = now + oauth2.expires_in * 1000 - 60000
  window.localStorage.setItem('access_token', token)
  window.localStorage.setItem('access_token_ttl', expires)
  return { token, expires }
}

//
//
const accessTokenExchange = async oauth2 => (
  fetch(OAUTH2_TOKEN, {
    method: 'POST',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    body: encode({ grant_type: 'authorization_code', client_id: OAUTH2_CLIENT_ID, code: oauth2.code }),
  })
    .then(jsonify)
    .then(accessTokenImplicit)
)

const jsonify = (http) => {
  if (http.ok) {
    return http.json()
  }
  throw Object.assign(new Error(`${http.status}\n${http.url}`),
    { type: `https://httpstatuses.com/${http.status}`, status: http.status })
}

//
//
const refresh = ({ expires }, { failed }) => {
  const now = +new Date()
  const timeout = expires - now
  setTimeout(() => failed('expired'), timeout)
}


//
// Network I/O
export const secureIO = (url, spec) => {
  const token = window.localStorage.getItem('access_token')
  const { headers, ...other } = spec || {}
  const { ['Authorization']: _, ...heads } = headers || {}
  return fetch(url, { ...other, headers: { ...heads, Authorization: token, 'Accept': 'application/json' } }).then(jsonify)
}

export const secureLookup = url => {
  const token = window.localStorage.getItem('access_token')
  const method = 'GET'
  const headers = { 'Authorization': token, 'Accept': 'application/json' }
  return fetch(url, {method, headers}).then(jsonify)
}

export const secureCreate = (url, json) => {
  const token = window.localStorage.getItem('access_token')
  const method = 'POST'
  const headers = { 'Authorization': token, 'Content-Type': 'application/json' }
  const body = JSON.stringify(json)
  return fetch(url, {method, headers, body}).then(jsonify)
}

export const secureUpdate = (url, json) => {
  const token = window.localStorage.getItem('access_token')
  const method = 'PUT'
  const headers = { 'Authorization': token, 'Content-Type': 'application/json' }
  const body = JSON.stringify(json)
  return fetch(url, {method, headers, body}).then(jsonify)
}

export const secureRemove = url => {
  const token = window.localStorage.getItem('access_token')
  const method = 'DELETE'
  const headers = { 'Authorization': token, 'Accept': 'application/json' }
  return fetch(url, {method, headers}).then(jsonify)
}
