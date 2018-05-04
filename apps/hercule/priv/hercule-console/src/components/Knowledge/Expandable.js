import React from 'react'
import { withStateHandlers } from 'recompose'
import { Table, TBody, TR, TD, Button } from 'react-dress-code'


const Expandable = ({list, expanded, toggle}) => (
  <TD>
    {!expanded &&
      <React.Fragment>
        {list[0]}
        <Button small link onClick={toggle}><i className="fa fa-angle-double-right" aria-hidden="true"></i></Button>
      </React.Fragment>
    }

    {expanded &&
      <Table>
      <TBody>
        <TR>
        <TD>
          {list[0]}
          <Button small link onClick={toggle}><i className="fa fa-angle-double-left" aria-hidden="true"></i></Button>
        </TD>
        </TR>
        {list.slice(1).map((x, i) => <TR key={i} tight><TD>{x}</TD></TR>)}
      </TBody>
      </Table>
    }
  </TD>
)


const ExpandableWithState = withStateHandlers(
  {
    expanded: false
  },
  {
    toggle: ({ expanded }) => () => ({expanded: !expanded})
  }
)(Expandable)


export default ExpandableWithState