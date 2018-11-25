import React from 'react'
import { Page, Container } from 'react-dress-code'


const Screen = props => (
  <Page>
    <Container limited>
      {props.children}
    </Container>
  </Page>
)

export default Screen
