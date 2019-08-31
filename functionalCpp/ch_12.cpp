#include <algorithm>
#include <functional>
#include <numeric>
#include <utility>
#include <boost/asio.hpp>
#include <json/json.h>

// Ch 12 Funcitonal design for concurrent systems
// Instead of having immutable data that is shared,
// we can have mutable data that is never shared

// We don't need to modify other objects, we can
// send them messages and let them react to those
// messages

// Actor model: actors are isolated entities
// that can't share anything but can send messages
// to each other. They should know who they're
// sending to and have multiple message types

// In a simplified version, we can define a single
// message type and let an externel controller
// deal with the message routing

// Receives SourceMessage, sends Message
template <typename SourceMessageType,
          typename MessageType>
class Actor
{
  public:
    // Make the type accessible
    using value_type = MessageType;
    void process_message(SourceMessageType&& message);

    // Sets the handler the actor calls when it wants
    // to send a message
    template <typename EmitFunction>
    void set_message_handler(EmitFunction emit); 

  private:
    std::function<void(MessageType&&)> m_emit;
};

// Sinks: actors that just receive messages
// Sources: actors that just send messages
// General actors: actors that do both

// Creating a simple message source
// Bookmark service
// Service - gets input from multiple sources and emits
// those messages as a unified stream of data

class service
{
  private:
    tcp::acceptor m_acceptor;
    tcp::socket   m_socket;
    std::function<void(std::string&&)> m_emit;

    void do_accept()
    {
      // We can accept multiple clients. We must be able to add them
      m_acceptor.async_accept(m_socket,
          [this] (const error_code& error)
          {
            if (!error)
              // Creates a new session for the accepte dclient
              make_shared_session(std::move(m_socket), m_emit)->start();
            else
              std::cerr << error.message() << std::endl;
            do_accept();
          });

    }
  public:
    using value_type = std::string;

    explicit service(boost::asio::io_service& service,
                     unsigned short port = 42042)
      : m_acceptor(service, tcp::endpoints(tcp::v4(), port)),
        m_socket(service);
    {
    }
    // Disable the copy constructor
    service(const service& other) = delete;
    // Use the default move constructor
    service(service&& other) = default;

    template <typename EmitFunction>
    void set_message_handler(EmitFunction emit)
    {
      m_emit = emit;
      // Only accept once we've been given a message handler
      do_accept();
    }
};

// A session should keep its own lifetime. If there's an error and the client
// disconnects, the session should destroy itslef.
// Session object can inherit from std::enable_shared_from_this.
// This allows the session to create a shared pointer on itself whenever a client
// connects. Once no clients are connected, it'll desctruct.

template <typename EmitFunction>
class session : public std::enable_shared_from_this<session<EmitFunction>>
{
  private:
    tcp::socket m_socket;
    boost::asio::streambuf m_data;
    EmitFunction m_emit;


    using shared_session =
      std::enable_shared_from_this<session<EmitFunction>>;
    void do_read()
    {
      // Creates another pointer to self
      auto self = shared_session::shared_from_this();
      // Schedules lambda to be executed when we get to a newline
      // in the input
      boost::asio::async_read_until(m_socket, m_data, '\n',
          [this, self] (const error_code& error, std::size_t size)
          {
            if (!error)
            {
              // Read the line and send the line to whoever registered
              // to listen for messages
              std::istream is(&m_data);
              std::string line;
              std::getline(is,line);
              m_emit(std::move(line));
              do_read();
            }
          });
    }

  public:
    session(tcp::socket&& socket, EmitFunction emit)
      : m_socket(std::move(socket)), m_emit(emit)
    {
    }

    void start()
    {
      do_read();
    }
};

// Modelling reactive streams as monads
// Async/reactive streams ar elike futures except
// with a singly linked list. However, you do not
// get the values all at once, you get them sequentially

// Requirements for a monad:
// - Must be a generic type
// - Need a constructor. Example: function that returns a
// reactive stream that contains a given value
// - Need the transformation function - function that returns
// a reactive stream that emits transformed values coming from
// the source stream
// - Need a join function that takes all messages from all given
// streams and emits them one by one
// - Need it to obey the monad laws (? need to read more into this)

// Creating a sink to receive messages
// Sink only needs to process messages

// Generic sink, takes a generic function to process messages
//
// This is single-owner design. The sink takes ownership of the sender.
// Although this limits the system so you can't have multiple components
// listen to a single actor, this can be fixed by using a shared_ptr.
// The benefit to this is that the chain of messaging will contain all of
// the actors in it. So when something is destroyed, it automatically destroys
// parts of the chain that can no longer be used.
namespace detail
{
  // We can use range notation to make the interface cleaner
  // Need a helper data structure
  template <typename Function>
  struct sink_helper
  {
    Function function
  };

  template <typename Sender, typename Function,
            typename MessageType = typename Sender::value_type>
  class sink_impl
  {
    public:
      sink_impl(Sender&& sender, Function function) 
        : m_sender(std::move(sender)), m_function(function)
      {
        // Assign our message handler to our sender
        m_sender.set_message_handler(
            [this] (MessageType&& message)
            {
              process_message(std::move(message));
            });
      }

      void process_message(MessageType&& message) const
      {
        // Hand over our message to the function for processing
        std::invoke(m_function, std::move(message));
      }
    private:
      Sender m_sender;
      Function m_function;
  };

  template <typename Sender, typename Transformation,
            typename SourceMessageType = typename Sender::value_type;
            typename MessageType =
              decltype(stdd::declval<Transformation>() (std::declval<SourceMessageType>()))>
  class transform_impl
  {
    public:
      using value_type = MessageType;

      transform_impl(Sender&& sender, Transformation transformation)
        : m_sender(std::move(sender)), m_transformation(transformation)
      {
      }

      template <typename EmitFunction>
      void set_message_handler(EmitFunction emit)
      {
        m_emit = emit;
        // Connect to the actor once the actor wants their messages
        // transformed. This meanas we now have someone to listen
        // to our messages
        m_sender.set_message_handler(
            [this] (SourceMessageType&& message)
            {
              process_message(std::move(message));
            });
      }

      void process_message(SourceMessageType&& message) const
      {
        // When processing a message, hand it over to the
        // transformation function
        m_emit(std::invoke(m_transformation, std::move(message)));
      }
    private:
      Sender m_sender;
      Transformation m_transformation;
      std::function<void(MessageType&&)> m_emit;
  };
}

// This takes a generic Sender class ang a _helper
// class that tells us the transformation that will be made
template <typename Sender, typename Function>
auto operator| (Sender&& sender,
                detail::sink_helper<Function> sink)
{
  return detail::sink_impl<Sender, Function>(
      std::forward<Sender>(sender), sink.function);
}

// Creating a sink_impl
template <typename Sender, typename Function>
auto sink(Sender&&, Function&& function)
{
  return detail::sink_impl<Sender, Function>(
      std::forward<Sender>(sender),
      std::forward<Function>(function);
}

int main(int argc, char** argv)
{
  // io_service listens for events and calls appropriate callback
  // lambdas for them
  boost::asio::io_service event_loop;
  /*
  auto pipeline = sink(service(event_loop),
      [] (const auto& message)
      {
        std::cerr << message << std::endl;
      });
  */

  // Can do this instead
  auto sink_to_cerr = sink([] (const auto& message)
      {
        std::cerr << message << std::endl;
      });
  auto pipeline = service(event_loop) | sink_to_cerr;
  event_loop.run();

  return 0;
}
