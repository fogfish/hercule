import React from 'react'
import { Button, Icon } from 'react-dress-code'

export const Deduct = ({ loading, fetchKnowledge }) => (
  <Button 
    primary 
    disabled={loading} 
    onClick={fetchKnowledge}
  >
    Deduct
    {loading 
      ? <Icon id="spinner" spin button left />
      : <Icon id="line-chart" button left />
    }
  </Button>
)
