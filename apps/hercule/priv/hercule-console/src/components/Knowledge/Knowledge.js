import React from 'react'
import { bindActionCreators } from 'redux'
import { connect } from 'react-redux'
import { lifecycle } from 'recompose'
import { Row, Column, Card, Table, THead, TBody, TR, TH, TD, Button } from 'react-dress-code'

const Knowledge = ({keys, knowledge}) => (
  <Row>
    <Column large={12} medium={12} small={12}>
      {keys.length > 0 &&
      <Card>
        <Table>
          <THead>
            <TR>
              {keys.map((x) => <TH>{x}</TH>)}
            </TR>
          </THead>
          <TBody>
            {knowledge.map(
              (k) => 
                <TR>
                  {keys.map((x) => <TD>{k[x]}</TD>)}
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
