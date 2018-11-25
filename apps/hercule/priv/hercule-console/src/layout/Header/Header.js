import React from 'react'
import { Tab, TabElement, TabGroup, Button } from 'react-dress-code'
import { authorize } from 'toolkit/OAuth2'

const Header = props => (
  <Tab>
    <TabElement header>
      <i className="fa fa-eercast" aria-hidden="true"></i> Hercule
    </TabElement>

    <TabGroup>
      <TabElement>
        <Button small link onClick={authorize}>Sign Out</Button>
      </TabElement>
    </TabGroup>
  </Tab>
)

export default Header
