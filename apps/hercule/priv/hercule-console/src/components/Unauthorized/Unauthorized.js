import React from 'react'
import { Toast, Link } from 'react-dress-code'
import './Unauthorized.css'
import { authorize } from 'toolkit/OAuth2'

const Unauthorized = ({ unauthorized }) => (
  <Toast error>
    Security session is {unauthorized}! 
    Authentication required! <Link link onClick={authorize}>Please sign-in</Link>.  
  </Toast>
)

export default Unauthorized