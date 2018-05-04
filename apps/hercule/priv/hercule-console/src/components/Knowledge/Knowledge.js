import React from 'react'
import { bindActionCreators } from 'redux'
import { connect } from 'react-redux'
import { Row, Column, Card, Table, THead, TBody, TR, TH, TD, Button } from 'react-dress-code'
import Expandable from './Expandable'
import { Link } from 'react-router-dom'


const Value = ({schema, data}) => (
  Array.isArray(data) && data.length > 1 
    ? <Expandable list={data} /> 
    : schema.startsWith('@')
    ? <TD><Link to={`/entity/${data}`}>{data}</Link></TD>
    : <TD>{data}</TD>
)

const Knowledge = ({keys, knowledge}) => (
  <Row>
    <Column large={12} medium={12} small={12}>
      {keys.length > 0 &&
      <Card>
        <Table>
          <THead>
            <TR>
              {keys.map((x, i) => <TH key={i}>{x}</TH>)}
            </TR>
          </THead>
          <TBody>
            {knowledge.map(
              (k, i) => 
                <TR key={i}>
                  {keys.map((x, j) => <Value key={j} schema={x} data={k[x]} />)}
                </TR>
            )}
          </TBody>
        </Table>
      </Card>
    }
    </Column>
  </Row>
)

const model = state => (state.api)
const actions = dispatch => bindActionCreators({  }, dispatch)
export default connect(model, actions)(Knowledge)
