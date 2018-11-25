import React from 'react'
import { TextArea } from 'react-dress-code'

export const CutQuery = ({ loading, query, updateQuery }) => (
  <TextArea
    cols="30"
    rows="8"
    value={query} 
    onChange={updateQuery}
    disabled={loading}
  />
)