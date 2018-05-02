import React from 'react'
import { Page, Container, Row, Column } from 'react-dress-code'
import Header from './Header'
import {withRouter} from 'react-router-dom'
import './Layout.css'


const Layout = (props) => (
  <Page>
    <Container limited>
      <Header />
        {props.children}
      </Container>
  </Page>
)

export default withRouter(Layout)
