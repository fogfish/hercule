import React from 'react'
import { bindActionCreators } from 'redux'
import { connect } from 'react-redux'
import { Tab, TabElement, Input } from 'react-dress-code'
import { Link } from 'react-router-dom'
import { withRouter } from 'react-router-dom'
import { setBucket } from '../../ducks/api'


const Header = ({location, api, setBucket}) => (
  <Tab>
    <TabElement header>
      <i className="fa fa-cloud" aria-hidden="true"></i> Console
    </TabElement>
    <TabElement active={location.pathname === "/"}><Link to="/">Datalog</Link></TabElement>
    <TabElement active={location.pathname === "/history"}><Link to="/history">History</Link></TabElement>


    <TabElement right>
      <i className="fa fa-cubes" aria-hidden="true"></i> Bucket
      <Input small defaultValue={api.bucket} onChange={(e) => setBucket(e.target.value) }/>
    </TabElement>
  </Tab>
)

const model = state => (state)
const actions = dispatch => bindActionCreators({ setBucket }, dispatch)
export default withRouter(connect(model, actions)(Header))
