
//
//
export const SCHEMA = "@@api/schema"
export const METADATA = "@@api/metadata"
export const KNOWLEDGE = "@@api/knowledge"
export const DATALOG = "@@api/datalog"
export const HISTORY = "@@api/history"
export const BUCKET = "@@api/bucket"
export const ENTITY = "@@api/entity"

const empty = {
  bucket: "nt",
  schema: [],
  keys: [],
  knowledge: [],
  datalog: "",
  history: [],
  entity: undefined
}

export default (state = empty, action) => {
  switch (action.type) {
    case SCHEMA:
      return { ...state, schema: action.schema }
    case KNOWLEDGE:
      return { ...state, keys: action.keys, knowledge: action.knowledge }
    case DATALOG:
      return { ...state, datalog: action.query }
    case HISTORY:
      return { ...state, history: state.history.concat([action.query]) }
    case BUCKET:
      return { ...state, bucket: action.bucket }
    case ENTITY:
      return { ...state, entity: action.entity }
    default:
      return { ...state }
  }
}



const jsonify = async (http) => {
  if (http.ok) { return http.json() }
  throw new Error({ type: `https://httpstatuses.com/${http.status}`, status: http.status })
}

const pubRead = url => (
    fetch(url, {
      method: 'GET',
    }).then(jsonify)
  )

// const pubSend = (url, body) => (
//     fetch(url, {
//       method: 'POST',
//       headers: {
//         'Content-Type': 'application/json',
//       },
//       body: JSON.stringify(body)
//     }).then(jsonify)
//   )

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
      const bucket = getState().api.bucket
      const schema = await pubRead(`http://localhost:8080/buckets/${bucket}`)
      dispatch({type: SCHEMA, schema})
   }

export const fetchKnowledge = () =>
  async (dispatch, getState) => {
    const bucket = getState().api.bucket
    const query = getState().api.datalog
    const knowledge = await datalog(`http://localhost:8080/buckets/${bucket}/deduct`, query)
    if (knowledge.length > 0)
    {
      const keys = Object.keys(knowledge[0])
      dispatch({type: KNOWLEDGE, keys, knowledge})
      dispatch({type: HISTORY, query})
    } else {
      alert('Not found!')
    }
  }

export const fetchEntity = (id) =>
  async (dispatch, getState) => {
      const bucket = getState().api.bucket
      const entity = await pubRead(`http://localhost:8080/buckets/${bucket}/keys/${id}`)
      dispatch({type: ENTITY, entity})    
  }

export const setDatalog = (query) =>
  (dispatch, getState) => {
    dispatch({type: DATALOG, query})
  }

export const setBucket = (bucket) =>
  (dispatch, getState) => {
    dispatch({type: BUCKET, bucket})
  }
