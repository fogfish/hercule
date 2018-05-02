import React from 'react'
import { bindActionCreators } from 'redux'
import { connect } from 'react-redux'
import { lifecycle } from 'recompose'
import { Row, Column, Card, Label } from 'react-dress-code'
import { Table, THead, TBody, TR, TH, TD, Button } from 'react-dress-code'

import { fetchSchema } from '../../ducks/api'


const Schema = ({schema}) => (
  <Table responsive>
  <THead>
    <TR tight>
      <TH>P</TH>
      <TH>Type</TH>
    </TR>
  </THead>
  <TBody>
    {schema.map((x) => <TR interactive tight><TD>{x.key}</TD><TD>{x.type}</TD></TR>)}
  </TBody>
  </Table>
)

const SchemaWithData = lifecycle({
   componentWillMount() {
      this.props.fetchSchema()
   }
})(Schema)


const model = state => (state.api)
const actions = dispatch => bindActionCreators({ fetchSchema }, dispatch)
export default connect(model, actions)(SchemaWithData)
