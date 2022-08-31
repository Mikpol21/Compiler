module Environment where
import Types
import Data.Map
import Control.Monad.Reader

type Env a = Reader (Map String a)



lookup :: MonadError e m => Id -> Env a -> m a
lookup = 