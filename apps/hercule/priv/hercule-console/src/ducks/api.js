
export const SCHEMA = "@@api/schema"
export const METADATA = "@@api/metadata"
export const KNOWLEDGE = "@@api/knowledge"
export const DATALOG = "@@api/datalog"
export const HISTORY = "@@api/history"


const jsonify = async (http) => {
  if (http.ok) { return http.json() }
  throw new Error({ type: `https://httpstatuses.com/${http.status}`, status: http.status })
}

const pubRead = url => (
    fetch(url, {
      method: 'GET',
    }).then(jsonify)
  )

const pubSend = (url, body) => (
    fetch(url, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(body)
    }).then(jsonify)
  )

const datalog = (url, datalog) => (
    fetch(url, {
      method: 'POST',
      headers: {
        'Content-Type': 'text/plain',
      },
      body: datalog
    }).then(jsonify)
  )


export const fetchSchema = () =>
   async (dispatch, getState) => {
      const schema = await pubRead("http://localhost:8080/_sys/schema")
      dispatch({type: SCHEMA, schema})
   }

export const fetchKnowledge = () =>
  async (dispatch, getState) => {
    const query = getState().api.datalog
    const knowledge = await datalog("http://localhost:8080/deduct", query)
    dispatch({type: METADATA, keys: Object.keys(knowledge[0])})
    dispatch({type: KNOWLEDGE, knowledge})
    dispatch({type: HISTORY, query})
  }

export const setDatalog = (query) =>
  (dispatch, getState) => {
    dispatch({type: DATALOG, query})
  }


//
//
const empty = {
   schema: [],
   keys: [],
   knowledge: [],
   datalog: "",
   history: []
}

export default (state = empty, action) => {
  switch (action.type) {
    case SCHEMA:
      return { ...state, schema: action.schema }
    case METADATA:
      return { ...state, keys: action.keys }
    case KNOWLEDGE:
      return { ...state, knowledge: action.knowledge }
    case DATALOG:
      return { ...state, datalog: action.query }
    case HISTORY:
      return { ...state, history: state.history.concat([action.query]) }
    default:
      return { ...state }
  }
}
