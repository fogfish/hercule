import React from 'react'
import { bindActionCreators } from 'redux'
import { connect } from 'react-redux'
import { lifecycle } from 'recompose'
import { withRouter } from 'react-router-dom'
import { Row, Column, Card, Table, TBody, TR, TD, H3 } from 'react-dress-code'

import { fetchEntity } from '../../ducks/api'



const Entity = ({entity}) => (
  <Row>
    <Column large={12} medium={12} small={12}>
      <Card>
        {entity && <H3>{entity.s}</H3>}
        <Table>
          <TBody>
            {entity && Object.keys(entity).map(
              (key, i) => {
                if (Array.isArray(entity[key]) && entity[key].length > 1) {
                  return entity[key].map(
                    (y, j) => <TR key={j}><TD>{j === 0 ? key : ''}</TD><TD>{y}</TD></TR>
                  )
                } else {
                  return <TR key={i}><TD>{key}</TD><TD>{entity[key]}</TD></TR>
                }
              }
            )}
          </TBody>
        </Table>
      </Card>
    </Column>
  </Row>
)

const EntityWithData = lifecycle({
  componentWillMount() {
    const id = this.props.match.params.id 
    this.props.fetchEntity(id)
  },

  componentWillReceiveProps() {
    const id = this.props.match.params.id 
    this.props.fetchEntity(id)    
  }
})(Entity)

const model = state => (state.api)
const actions = dispatch => bindActionCreators({ fetchEntity }, dispatch)
export default withRouter(connect(model, actions)(EntityWithData))