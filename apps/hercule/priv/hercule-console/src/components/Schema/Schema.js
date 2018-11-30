import React from 'react'
import { compose, withState, lifecycle } from 'recompact'
import { Table, THead, TBody, TR, TH, TD } from 'react-dress-code'
import { secureLookup } from 'toolkit/OAuth2'

const Schema = ({ schema }) => (
  <Table responsive>
    <THead>
      <TR tight>
        <TH>P</TH>
        <TH>Type</TH>
      </TR>
    </THead>
    <TBody>
      {Object.keys(schema).map(
        (key) => <TR key={key} interactive tight><TD>{key}</TD><TD>{schema[key]}</TD></TR>
      )}
    </TBody>
  </Table>
)

const SchemaWithData = compose(
  withState('schema', 'setSchema', {}),
  lifecycle({
    componentWillMount() {
      secureLookup('/hercule/buckets/*')
        .then(this.props.setSchema)
        .catch(() => null)
    }
  })
)(Schema)

export default SchemaWithData
