import React from 'react'
import {
  BrowserRouter as Router,
  Route
} from 'react-router-dom'
import './App.css'


import { Layout } from './components/Layout'
import { VisualLog } from './components/VisualLog'
import { HistoryLog } from './components/HistoryLog'
import { Knowledge } from './components/Knowledge'
import { Entity } from './components/Entity'

const App = () => (
  <div>
    <Router>
      <Layout>
        <Route exact path="/console" component={VisualLog} />
        <Route exact path="/console/history" component={HistoryLog} />
        <Route exact path="/console/entity/:id" component={Entity} />
        <Route path="/" component={Knowledge} />
      </Layout>
    </Router>
  </div>
)

export default App
