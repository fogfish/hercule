// HoC
import { compose, withState, withHandlers, lifecycle } from 'recompact'
import { secureIO } from 'toolkit/OAuth2'

export const Datalog = compose(
  withState('query', 'setQuery', ''),
  withState('knowledge', 'setKnowledge', []),
  withState('loading', 'setLoading', false),
  withState('failed', 'setError', undefined),
  withHandlers({
    updateQuery: props => e => (
      props.setQuery(e.target.value)
    ),
    fetchKnowledge: props => async _ => {
      props.setLoading(true)
      props.setKnowledge([])
      props.setError(undefined)
      try {
        const k = await secureIO('http://localhost:8080/hercule/deduct', {
          method: 'POST',
          headers: {'Content-Type': 'text/plain'},
          body: props.query
        })
        props.setKnowledge(k)
        props.setLoading(false)
      } catch (e) {
        props.setKnowledge([])
        props.setLoading(false)
        props.setError(e)
        throw e
      }
    }
  })
)

export const Knowledge = lifecycle({
  shouldComponentUpdate(props) {
    // Note: rendering of knowledge is heavy ops, we do it only once when component is mounted
    return ((this.props.knowledge.length != props.knowledge.length) || (props.failed !== undefined) || (this.props.failed !== props.failed))
  }
})
