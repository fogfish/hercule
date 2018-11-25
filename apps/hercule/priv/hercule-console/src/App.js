import React from 'react'
import './App.css'

import { Screen } from 'layout/Screen'
import { Header } from 'layout/Header'
import { Unauthorized } from 'components/Unauthorized'
import { Query } from 'components/Query'
import { OAuth2 } from 'toolkit/OAuth2'


const Private = OAuth2(() => (null))(Unauthorized)(Query)

const App = () => (
  <React.Fragment>
    <Screen>
      <Header />
      <Private />
    </Screen>
  </React.Fragment>
)

export default App
