from flask import Flask, request
from flask_restful import Api, Resource, reqparse, abort, fields, marshal_with
from flask_sqlalchemy import SQLAlchemy

app = Flask(__name__)
api = Api(app)
app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite:///database.db'
db = SQLAlchemy(app)

class VideoModel(db.Model):
    #Flask sql database
    #Define all the field you want to have in the model
    id = db.Column(db.Integer, primary_key = True)
    name = db.Column(db.String(100), nullable=False)
    views = db.Column(db.Integer, nullable=False)
    likes = db.Column(db.Integer, nullable=False)

    def __repr__(self):
        return f"Video(name = {name}, views = {views}, likes = {likes})"

db.create_all() #run only once

video_put_args = reqparse.RequestParser()
video_put_args.add_argument("name", type=str, help="Name of the video", required=True)
video_put_args.add_argument("likes", type=int, help="Likes of the video", required=True)
video_put_args.add_argument("views", type=int, help="Views of the video", required=True)

video_update_args = reqparse.RequestParser()
video_update_args.add_argument("name", type=str, help="Name of the video")
video_update_args.add_argument("likes", type=int, help="Likes of the video")
video_update_args.add_argument("views", type=int, help="Views of the video")


names = {"chris" : {"age": 25, "gender":"male"},
        "john" : {"age" : 30, "gender" : "male"}
        }
class HelloWorld(Resource):
    def get(self, name):
        return names[name]

api.add_resource(HelloWorld, "/helloworld/<string:name>")

#videos = {}

# def abort_if_video_id_doesnt_exist(video_id):
#     if video_id not in videos:
#         abort(404, message = "Video id is not valid")

# def abort_if_video_exists(video_id):
#     if video_id in videos:
#         abort(409, message="Video already exists with that ID...")

resource_fields = {
    'id' : fields.Integer,
    'name': fields.String,
    'views' : fields.Integer,
    'likes' : fields.Integer
}

class Video(Resource):
    @marshal_with(resource_fields) #parse object, serialize into json format
    def get(self, video_id):
        #abort_if_video_id_doesnt_exist(video_id) #to avoid crashing program if video id doesnt exist
        #return videos[video_id]
        
        result = VideoModel.query.filter_by(id=video_id).first() #instance of an object
        if not result:
            abort(404, message = "could not find video with that id")
        return result
        
    @marshal_with(resource_fields)
    def put(self, video_id):
        # abort_if_video_exists(video_id)
        # args = video_put_args.parse_args()
        # videos[video_id] = args
        #print(request.form) #get the form json
        args = video_put_args.parse_args() #returns a dict that stores the form passed in
        result = VideoModel.query.filter_by(id = video_id).first()
        if result:
            #if able to query a result based on the id, abort
            abort(409, message = "Video id taken..")

        video = VideoModel(id = video_id, name = args['name'], views = args['views'], likes = args['likes'])
        db.session.add(video) #insert into database
        db.session.commit() #commit the insertion
        return video, 201
    
    @marshal_with(resource_fields)
    def patch(self, video_id):
        args = video_update_args.parse_args()
        result = VideoModel.query.filter_by(id=video_id).first()
        if not result:
            abort(404, message="Video doesnt exist, cannot update")

        if args["name"]: #if not none
            result.name = args['name']
        if args["views"]:
            result.views = args["views"]
        if args["likes"]:
            result.likes = args["likes"]

        #db.session.add(result)
        db.session.commit()

        return result

    def delete(self, video_id):
        abort_if_video_id_doesnt_exist(video_id)
        del videos[video_id]
        return '', 204
api.add_resource(Video, "/video/<int:video_id>")

if __name__ == "__main__":
    app.run(debug=True)