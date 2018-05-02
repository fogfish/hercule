import React from 'react'
import { bindActionCreators } from 'redux'
import { connect } from 'react-redux'
import { Tab, TabElement, Input, Button } from 'react-dress-code'
import { Link } from 'react-router-dom'
import {withRouter} from 'react-router-dom'


const Header = ({location}) => (
  <Tab>
    <TabElement header>
      <i className="fa fa-cloud" aria-hidden="true"></i> Console
    </TabElement>
    <TabElement active={location.pathname === "/"}><Link to="/">Datalog</Link></TabElement>
    <TabElement active={location.pathname === "/history"}><Link to="/history">History</Link></TabElement>


    <TabElement right>
      <Button small link><i className="fa fa-cubes" aria-hidden="true"></i> Connect</Button>
      <Input small defaultValue="http://localhost:9200/nt"/>
    </TabElement>
  </Tab>
)

const model = state => (state)
const actions = dispatch => bindActionCreators({}, dispatch)
export default withRouter(connect(model, actions)(Header))
