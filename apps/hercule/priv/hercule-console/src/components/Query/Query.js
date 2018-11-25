import React from 'react'

import { Row, Column, Card, Label, SideRevealer } from 'react-dress-code'
import { Datalog } from 'components/Datalog'
import { Schema } from 'components/Schema'
import { Deduct } from './Deduct'
import { CutQuery } from './CutQuery'
import { Knowledge } from './Knowledge'

const Query = props => (
  <React.Fragment>
    <Row>
      <Column large={12} medium={12} small={12}>
        <SideRevealer side={() => (<Schema />)}>
        <Card>
          <Label>Datalog</Label>
          <CutQuery { ...props } />
          <Deduct { ...props } />
        </Card>
        </SideRevealer>
      </Column>
    </Row>
    <Knowledge { ...props } />
  </React.Fragment>
)

export default Datalog(Query)
