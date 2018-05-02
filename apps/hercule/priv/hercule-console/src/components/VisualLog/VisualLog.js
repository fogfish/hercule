import React from 'react'
import { bindActionCreators } from 'redux'
import { connect } from 'react-redux'
import { Row, Column, Card, Label, SideRevealer, Button, Tab, TabElement } from 'react-dress-code'
import Schema from './Schema'
import { fetchKnowledge, setDatalog } from '../../ducks/api'

const Side = () => (
  <div><Schema /></div>
)

const VisualLog = ({schema, datalog, fetchKnowledge, setDatalog}) => (
  <Row>
    <Column large={12} medium={12} small={12}>
      <SideRevealer side={Side}>
      <Card>
        <Label>Datalog</Label>
        <textarea className="dc-textarea" cols="30" rows="8" value={datalog} onChange={(e) => setDatalog(e.target.value)}></textarea>
        <Button primary onClick={fetchKnowledge}>Deduct</Button>
      </Card>
      </SideRevealer>
    </Column>
  </Row>
)


const model = state => (state.api)
const actions = dispatch => bindActionCreators({ fetchKnowledge, setDatalog }, dispatch)
export default connect(model, actions)(VisualLog)
