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

const App = () => (
  <div>
    <Router>
      <Layout>
        <Route exact path="/" component={VisualLog} />
        <Route exact path="/history" component={HistoryLog} />
        <Route path="/" component={Knowledge} />
      </Layout>
    </Router>
  </div>
)

export default App
