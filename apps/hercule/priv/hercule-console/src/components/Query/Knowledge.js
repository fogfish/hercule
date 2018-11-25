import React from 'react'
import { Knowledge as Static } from 'components/Datalog'
import { Row, Column, Card, Table, THead, TBody, TR, TH, TD, Toast } from 'react-dress-code'

const Head = ({ columns }) => (
  <THead>
    <TR>
      {columns.map(x => <TH key={x}>{x}</TH>)}
    </TR>
  </THead>
)

const Body = ({ columns, knowledge }) => (
  <TBody>
    {knowledge.map(
      (k, i) => 
        <TR key={i}>
          {columns.map((x, j) => <TD key={j}>{k[x]}</TD>)}
        </TR>
      )
    }
  </TBody>
)

export const Knowledge = Static(
  ({ knowledge, failed }) => (
    <React.Fragment>
      {failed !== undefined &&
        <Toast error>Unable to execute query!</Toast>
      }
      {knowledge.length === 0 ? null :
        <Row>
          <Column large={12} medium={12} small={12}>
            <Card>
              <Table>
                <Head columns={Object.keys(knowledge[0])} />
                <Body columns={Object.keys(knowledge[0])} knowledge={knowledge} /> 
              </Table>
            </Card>
          </Column>
        </Row>
      }
    </React.Fragment>
  )
)
