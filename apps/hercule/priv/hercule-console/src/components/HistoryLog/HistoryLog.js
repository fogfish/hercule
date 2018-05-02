import React from 'react'
import { bindActionCreators } from 'redux'
import { connect } from 'react-redux'
import { Row, Column, Card, Label, Button, Divider } from 'react-dress-code'


const HistoryLog = ({history}) => (
  <Row>
    <Column large={12} medium={12} small={12}>
      {history.length > 0 &&
      <Card>
        <Label>History</Label>
        {history.map(
            (x) => <div><pre className="dc--text-small">{x}</pre><Divider secondary/></div>
         )}
      </Card>
      }

      {history.length === 0 &&
      <Card>
        <Label>No History</Label>
      </Card>
      }
    </Column>
  </Row>
)

const model = state => (state.api)
const actions = dispatch => bindActionCreators({  }, dispatch)
export default connect(model, actions)(HistoryLog)
