import React from 'react'
import { bindActionCreators } from 'redux'
import { connect } from 'react-redux'
import { lifecycle } from 'recompose'
import { Table, THead, TBody, TR, TH, TD } from 'react-dress-code'

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
    {Object.keys(schema).map(
      (key, i) => <TR key={i} interactive tight><TD>{key}</TD><TD>{schema[key]}</TD></TR>
    )}
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
