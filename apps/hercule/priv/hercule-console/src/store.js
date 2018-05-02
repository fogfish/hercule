//
//
import { createStore, applyMiddleware, combineReducers } from 'redux'
import { createLogger } from 'redux-logger'
import thunk from 'redux-thunk'

//
//
import { default as api } from './ducks/api'

const root = combineReducers({
   api
})

const logger = createLogger()
const store  = createStore(root, applyMiddleware(thunk, logger))
// const store  = createStore(root, applyMiddleware(thunk))

export default store
